// Copyright (C) 2020-2024 Free Software Foundation, Inc.

// This file is part of GCC.

// GCC is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free
// Software Foundation; either version 3, or (at your option) any later
// version.

// GCC is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or
// FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
// for more details.

// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#ifndef RUST_AST_CONDCOMPILATION
#define RUST_AST_CONDCOMPILATION
// Conditional compilation-related AST stuff

#include "rust-ast.h"

namespace Rust {
namespace AST {
// Base conditional compilation configuration predicate thing - abstract
class ConfigurationPredicate
{
public:
  virtual ~ConfigurationPredicate () {}

  // Unique pointer custom clone function
  std::unique_ptr<ConfigurationPredicate> clone_configuration_predicate () const
  {
    return std::unique_ptr<ConfigurationPredicate> (
      clone_configuration_predicate_impl ());
  }

  // not sure if I'll use this but here anyway
  virtual void accept_vis (ASTVisitor &vis) = 0;

protected:
  // Clone function impl to be overriden in base classes
  virtual ConfigurationPredicate *
  clone_configuration_predicate_impl () const = 0;
};

// A configuration option - true if option is set, false if option is not set.
class ConfigurationOption : public ConfigurationPredicate
{
  Identifier option_name;

  // bool has_string_literal_option_body;
  std::string option_value; // technically a string or raw string literal

public:
  /* Returns whether the configuration option has a "value" part of the
   * key-value pair. */
  bool has_option_value () const { return !option_value.empty (); }

  // Key-value pair constructor
  ConfigurationOption (Identifier option_name, std::string option_value)
    : option_name (option_name), option_value (option_value)
  {}

  // Name-only constructor
  ConfigurationOption (Identifier option_name) : option_name (option_name) {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConfigurationOption *clone_configuration_predicate_impl () const override
  {
    return new ConfigurationOption (*this);
  }
};

// TODO: inline
struct ConfigurationPredicateList
{
  std::vector<std::unique_ptr<ConfigurationPredicate>> predicate_list;
};

// Predicate that returns true if all of the supplied predicates return true.
class ConfigurationAll : public ConfigurationPredicate
{
  std::vector<std::unique_ptr<ConfigurationPredicate>>
    predicate_list; // inlined form

public:
  ConfigurationAll (
    std::vector<std::unique_ptr<ConfigurationPredicate>> predicate_list)
    : predicate_list (predicate_list)
  {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConfigurationAll *clone_configuration_predicate_impl () const override
  {
    return new ConfigurationAll (*this);
  }
};

// Predicate that returns true if any of the supplied predicates are true.
class ConfigurationAny : public ConfigurationPredicate
{
  std::vector<std::unique_ptr<ConfigurationPredicate>>
    predicate_list; // inlined form

public:
  ConfigurationAny (
    std::vector<std::unique_ptr<ConfigurationPredicate>> predicate_list)
    : predicate_list (predicate_list)
  {}

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConfigurationAny *clone_configuration_predicate_impl () const override
  {
    return new ConfigurationAny (*this);
  }
};

/* Predicate that produces the negation of a supplied other configuration
 * predicate. */
class ConfigurationNot : public ConfigurationPredicate
{
  std::unique_ptr<ConfigurationPredicate> config_to_negate;

public:
  ConfigurationNot (ConfigurationPredicate *config_to_negate)
    : config_to_negate (config_to_negate)
  {}

  // Copy constructor with clone
  ConfigurationNot (ConfigurationNot const &other)
    : config_to_negate (
      other.config_to_negate->clone_configuration_predicate ())
  {}

  // Overloaded assignment operator to clone
  ConfigurationNot &operator= (ConfigurationNot const &other)
  {
    config_to_negate = other.config_to_negate->clone_configuration_predicate ();

    return *this;
  }

  // move constructors
  ConfigurationNot (ConfigurationNot &&other) = default;
  ConfigurationNot &operator= (ConfigurationNot &&other) = default;

  void accept_vis (ASTVisitor &vis) override;

protected:
  /* Use covariance to implement clone function as returning this object rather
   * than base */
  ConfigurationNot *clone_configuration_predicate_impl () const override
  {
    return new ConfigurationNot (*this);
  }
};

// TODO: relationship to other attributes?
class CfgAttribute
{
  std::unique_ptr<ConfigurationPredicate> config_to_include;

public:
  CfgAttribute (ConfigurationPredicate *config_to_include)
    : config_to_include (config_to_include)
  {}

  // Copy constructor with clone
  CfgAttribute (CfgAttribute const &other)
    : config_to_include (
      other.config_to_include->clone_configuration_predicate ())
  {}

  // Overloaded assignment operator to clone
  CfgAttribute &operator= (CfgAttribute const &other)
  {
    config_to_include
      = other.config_to_include->clone_configuration_predicate ();

    return *this;
  }

  // move constructors
  CfgAttribute (CfgAttribute &&other) = default;
  CfgAttribute &operator= (CfgAttribute &&other) = default;
};
/* TODO: ok, best thing to do would be eliminating this class, making Attribute
 * has a "is_cfg()" method, and having attribute path as "cfg" and AttrInput as
 * ConfigurationPredicate (so make ConfigurationPredicate a subclass of
 * AttrInput?). Would need special handling in parser, however. */

// TODO: inline
struct CfgAttrs
{
  std::vector<Attribute> cfg_attrs;
};

// TODO: relationship to other attributes?
class CfgAttrAttribute
{
  std::unique_ptr<ConfigurationPredicate> config_to_include;
  std::vector<Attribute> cfg_attrs;

public:
  CfgAttrAttribute (ConfigurationPredicate *config_to_include,
		    std::vector<Attribute> cfg_attrs)
    : config_to_include (config_to_include), cfg_attrs (cfg_attrs)
  {}

  // Copy constructor with clone
  CfgAttrAttribute (CfgAttrAttribute const &other)
    : config_to_include (
      other.config_to_include->clone_configuration_predicate ()),
      cfg_attrs (cfg_attrs)
  {}

  // Overloaded assignment operator to clone
  CfgAttrAttribute &operator= (CfgAttrAttribute const &other)
  {
    config_to_include
      = other.config_to_include->clone_configuration_predicate ();
    cfg_attrs = other.cfg_attrs;

    return *this;
  }

  // move constructors
  CfgAttrAttribute (CfgAttrAttribute &&other) = default;
  CfgAttrAttribute &operator= (CfgAttrAttribute &&other) = default;
};
} // namespace AST
} // namespace Rust

#endif
