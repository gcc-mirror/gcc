#ifndef RUST_AST_CONDCOMPILATION
#define RUST_AST_CONDCOMPILATION
// Conditional compilation-related AST stuff

#include "rust-ast.h"

namespace Rust {
    namespace AST {
        // Base conditional compilation configuration predicate thing - abstract
        class ConfigurationPredicate {
          public:
            virtual ~ConfigurationPredicate() {}

            // Unique pointer custom clone function
            ::std::unique_ptr<ConfigurationPredicate> clone_configuration_predicate() const {
                return ::std::unique_ptr<ConfigurationPredicate>(
                  clone_configuration_predicate_impl());
            }

          protected:
            // Clone function impl to be overriden in base classes
            virtual ConfigurationPredicate* clone_configuration_predicate_impl() const = 0;
        };

        // A configuration option - true if option is set, false if option is not set.
        class ConfigurationOption : public ConfigurationPredicate {
            Identifier option_name;

            // bool has_string_literal_option_body;
            ::std::string option_value; // technically a string or raw string literal

          public:
            // Returns whether the configuration option has a "value" part of the key-value pair.
            inline bool has_option_value() const {
                return !option_value.empty();
            }

            // Key-value pair constructor
            ConfigurationOption(Identifier option_name, ::std::string option_value) :
              option_name(option_name), option_value(option_value) {}

            // Name-only constructor
            ConfigurationOption(Identifier option_name) : option_name(option_name) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ConfigurationOption* clone_configuration_predicate_impl() const OVERRIDE {
                return new ConfigurationOption(*this);
            }
        };

        // TODO: inline
        struct ConfigurationPredicateList {
            ::std::vector< ::std::unique_ptr<ConfigurationPredicate> > predicate_list;
        };

        // Predicate that returns true if all of the supplied predicates return true.
        class ConfigurationAll : public ConfigurationPredicate {
            ::std::vector< ::std::unique_ptr<ConfigurationPredicate> > predicate_list; // inlined form

          public:
            ConfigurationAll(
              ::std::vector< ::std::unique_ptr<ConfigurationPredicate> > predicate_list) :
              predicate_list(predicate_list) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ConfigurationAll* clone_configuration_predicate_impl() const OVERRIDE {
                return new ConfigurationAll(*this);
            }
        };

        // Predicate that returns true if any of the supplied predicates are true.
        class ConfigurationAny : public ConfigurationPredicate {
            ::std::vector< ::std::unique_ptr<ConfigurationPredicate> > predicate_list; // inlined form

          public:
            ConfigurationAny(
              ::std::vector< ::std::unique_ptr<ConfigurationPredicate> > predicate_list) :
              predicate_list(predicate_list) {}

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ConfigurationAny* clone_configuration_predicate_impl() const OVERRIDE {
                return new ConfigurationAny(*this);
            }
        };

        // Predicate that produces the negation of a supplied other configuration predicate.
        class ConfigurationNot : public ConfigurationPredicate {
            ::std::unique_ptr<ConfigurationPredicate> config_to_negate;

          public:
            ConfigurationNot(ConfigurationPredicate* config_to_negate) :
              config_to_negate(config_to_negate) {}

            // Copy constructor with clone
            ConfigurationNot(ConfigurationNot const& other) :
              config_to_negate(other.config_to_negate->clone_configuration_predicate()) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            ConfigurationNot& operator=(ConfigurationNot const& other) {
                config_to_negate = other.config_to_negate->clone_configuration_predicate();

                return *this;
            }

            // no move constructors as not supported in c++03
            /*ConfigurationNot(ConfigurationNot&& other) = default;
            ConfigurationNot& operator=(ConfigurationNot&& other) = default;*/

          protected:
            // Use covariance to implement clone function as returning this object rather than base
            virtual ConfigurationNot* clone_configuration_predicate_impl() const OVERRIDE {
                return new ConfigurationNot(*this);
            }
        };

        // TODO: relationship to other attributes?
        class CfgAttribute {
            ::std::unique_ptr<ConfigurationPredicate> config_to_include;

          public:
            CfgAttribute(ConfigurationPredicate* config_to_include) :
              config_to_include(config_to_include) {}

            // Copy constructor with clone
            CfgAttribute(CfgAttribute const& other) :
              config_to_include(other.config_to_include->clone_configuration_predicate()) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            CfgAttribute& operator=(CfgAttribute const& other) {
                config_to_include = other.config_to_include->clone_configuration_predicate();

                return *this;
            }

            // no move constructors as not supported in c++03
            /*CfgAttrAttribute(CfgAttrAttribute&& other) = default;
            CfgAttrAttribute& operator=(CfgAttrAttribute&& other) = default;*/
        };

        // TODO: inline
        struct CfgAttrs {
            ::std::vector<Attribute> cfg_attrs;
        };

        // TODO: relationship to other attributes?
        class CfgAttrAttribute {
            ::std::unique_ptr<ConfigurationPredicate> config_to_include;
            ::std::vector<Attribute> cfg_attrs;

          public:
            CfgAttrAttribute(
              ConfigurationPredicate* config_to_include, ::std::vector<Attribute> cfg_attrs) :
              config_to_include(config_to_include),
              cfg_attrs(cfg_attrs) {}

            // Copy constructor with clone
            CfgAttrAttribute(CfgAttrAttribute const& other) :
              config_to_include(other.config_to_include->clone_configuration_predicate()),
              cfg_attrs(cfg_attrs) {}

            // Destructor - define here if required

            // Overloaded assignment operator to clone
            CfgAttrAttribute& operator=(CfgAttrAttribute const& other) {
                config_to_include = other.config_to_include->clone_configuration_predicate();
                cfg_attrs = other.cfg_attrs;

                return *this;
            }

            // no move constructors as not supported in c++03
            /*CfgAttrAttribute(CfgAttrAttribute&& other) = default;
            CfgAttrAttribute& operator=(CfgAttrAttribute&& other) = default;*/
        };
    }
}

#endif