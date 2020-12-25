// Copyright (C) 2020 Free Software Foundation, Inc.

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

#ifndef CLONE_TEST_H
#define CLONE_TEST_H
// Potential fancy deep cloning test for use with unique_ptr

// disable by default
#if 0
#include <memory>

namespace clone_code {
    template<typename T>
    class abstract_method {};

    template<typename T>
    class virtual_inherit_from : virtual public T {
        using T::T;
    };

    template<typename Derived, typename... Bases>
    class clone_inherit : public Bases... {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      protected:
      private:
        virtual clone_inherit* clone_impl() const override {
            return new Derived(static_cast<const Derived&>(*this));
        }
    };

    template<typename Derived, typename... Bases>
    class clone_inherit<abstract_method<Derived>, Bases...> : public Bases... {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      protected:
      private:
        virtual clone_inherit* clone_impl() const = 0;
    };

    template<typename Derived>
    class clone_inherit<Derived> {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      private:
        virtual clone_inherit* clone_impl() const override {
            return new Derived(static_cast<const Derived&>(*this));
        }
    };

    template<typename Derived>
    class clone_inherit<abstract_method<Derived> > {
      public:
        virtual ~clone_inherit() = default;

        std::unique_ptr<Derived> clone() const {
            return std::unique_ptr<Derived>(static_cast<Derived*>(this->clone_impl()));
        }

      private:
        virtual clone_inherit* clone_impl() const = 0;
    };
}

namespace user_code {
    using namespace clone_code;

    class cloneable : public clone_inherit<abstract_method<cloneable> > {};

    class foo : public clone_inherit<abstract_method<foo>, virtual_inherit_from<cloneable> > {};

    class bar : public clone_inherit<abstract_method<bar>, virtual_inherit_from<cloneable> > {};

    class concrete : public clone_inherit<concrete, foo, bar> {};
}
#endif

#endif
