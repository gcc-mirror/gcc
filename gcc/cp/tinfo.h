// RTTI support internals for -*- C++ -*-
// Copyright (C) 1994, 1995, 1996, 1998 Free Software Foundation

#include "typeinfo"

// Class declarations shared between the typeinfo implementation files.

// type_info for a class with no base classes (or an enum).

struct __user_type_info : public std::type_info {
  __user_type_info (const char *n) : type_info (n) {}

  // If our type can be converted to the desired type, 
  // return the pointer, adjusted accordingly; else return 0.
  virtual void* dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};

// type_info for a class with one public, nonvirtual base class.

class __si_type_info : public __user_type_info {
  const __user_type_info &base;

public:
  __si_type_info (const char *n, const __user_type_info &b)
    : __user_type_info (n), base (b) { }

  virtual void *dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};

// type_info for a general class.

typedef unsigned int USItype	__attribute__ ((mode (SI)));

struct __class_type_info : public __user_type_info {
  enum access { PUBLIC = 1, PROTECTED = 2, PRIVATE = 3 };

  struct base_info {
    const __user_type_info *base;
    USItype offset: 29;
    bool is_virtual: 1;
    enum access access: 2;
  };

  const base_info *base_list;
  size_t n_bases;

  __class_type_info (const char *name, const base_info *bl, size_t bn)
    : __user_type_info (name), base_list (bl), n_bases (bn) {}

  // This is a little complex.
  virtual void* dcast (const type_info &, int, void *,
		       const type_info * = 0, void * = 0) const;
};
