// RTTI support internals for -*- C++ -*-
// Copyright (C) 1994, 1995, 1996, 1998, 1999, 2000 Free Software Foundation

#include "typeinfo"

// Class declarations shared between the typeinfo implementation files.

// type_info for a class with no base classes (or an enum).

struct __user_type_info : public std::type_info {
  __user_type_info (const char *n) : type_info (n) {}

  // If our type can be upcast to a public and unambiguous base, then return
  // non-zero and set RES to point to the base object. OBJ points to the throw
  // object and can be NULL, if there is no object to adjust.
  int upcast (const type_info &target, void *obj, void **res) const;
  
  // If our type can be dynamicly cast to the target type, then return
  // pointer to the target object. OBJ is the pointer to the most derived
  // type and cannot be NULL. SUBTYPE and SUBOBJ indicate the static type
  // base object from whence we came, it cannot be NULL. SUBTYPE cannot be
  // the same as TARGET. TARGET cannot be a base of SUBTYPE.
  // BOFF indicates how SUBTYPE is related to TARGET.
  // BOFF >= 0, there is only one public non-virtual SUBTYPE base at offset
  //    BOFF, and there are no public virtual SUBTYPE bases.
  //    Therefore check if SUBOBJ is at offset BOFF when we find a target
  // BOFF == -1, SUBTYPE occurs as multiple public virtual or non-virtual bases.
  //    Lazily search all the bases of TARGET.
  // BOFF == -2, SUBTYPE is not a public base.
  // BOFF == -3, SUBTYPE occurs as multiple public non-virtual bases.
  //    Lazily search the non-virtual bases of TARGET.
  // For backwards compatibility set BOFF to -1, that is the safe `don't know'
  // value. We don't care about SUBTYPES as private bases of TARGET, as they
  // can never succeed as downcasts, only as crosscasts -- and then only if
  // they are virtual. This is more complicated that it might seem.
  void *dyncast (int boff,
                 const type_info &target, void *obj,
                 const type_info &subtype, void *subobj) const;
  
  // non_virtual_base_type is used to indicate that a base class is via a
  // non-virtual access path.
  static const type_info *const nonvirtual_base_type
      = static_cast <const type_info *> (0) + 1;
  
  // sub_kind tells us about how a base object is contained within a derived
  // object. We often do this lazily, hence the UNKNOWN value. At other times
  // we may use NOT_CONTAINED to mean not publicly contained.
  enum sub_kind
  {
    unknown = 0,              // we have no idea
    not_contained,            // not contained within us (in some
                              // circumstances this might mean not contained
                              // publicly)
    contained_ambig,          // contained ambiguously
    contained_mask = 4,       // contained within us
    contained_virtual_mask = 1, // via a virtual path
    contained_public_mask = 2,  // via a public path
    contained_private = contained_mask,
    contained_public = contained_mask | contained_public_mask
  };
  // some predicate functions for sub_kind
  static inline bool contained_p (sub_kind access_path)
  {
    return access_path >= contained_mask;
  }
  static inline bool contained_public_p (sub_kind access_path)
  {
    return access_path >= contained_public;
  }
  static inline bool contained_nonpublic_p (sub_kind access_path)
  {
    return (access_path & contained_public) == contained_mask;
  }
  static inline bool contained_nonvirtual_p (sub_kind access_path)
  {
    return (access_path & (contained_mask | contained_virtual_mask))
           == contained_mask;
  }
  
  struct upcast_result
  {
    void *target_obj;   // pointer to target object or NULL (init NULL)
    sub_kind whole2target;      // path from most derived object to target
    const type_info *base_type; // where we found the target, (init NULL)
                                // if in vbase the __user_type_info of vbase)
                                // if a non-virtual base then 1
                                // else NULL
    public:
    upcast_result ()
      :target_obj (NULL), whole2target (unknown), base_type (NULL)
      {}
  };
  struct dyncast_result
  {
    void *target_obj;   // pointer to target object or NULL (init NULL)
    sub_kind whole2target;      // path from most derived object to target
    sub_kind whole2sub;         // path from most derived object to sub object
    sub_kind target2sub;        // path from target to sub object
    
    public:
    dyncast_result ()
      :target_obj (NULL), whole2target (unknown),
       whole2sub (unknown), target2sub (unknown)
      {}
  };
  
  public:
  // Helper for upcast. See if TARGET is us, or one of our bases. ACCESS_PATH
  // gives the access from the start object. Return TRUE if we know the catch
  // fails.
  virtual bool do_upcast (sub_kind access_path,
                          const type_info &target, void *obj,
                          upcast_result &__restrict result) const;
  // Helper for dyncast. BOFF indicates how the SUBTYPE is related to TARGET.
  // ACCESS_PATH indicates the access from the most derived object.  It is
  // used to prune the DAG walk. All information about what we find is put
  // into RESULT. Return true, if the match we have found is ambiguous.
  virtual bool do_dyncast (int boff, sub_kind access_path,
                           const type_info &target, void *obj,
                           const type_info &subtype, void *subptr,
                           dyncast_result &__restrict result) const;
  public:
  // Indicate whether SUBPTR of type SUBTYPE is contained publicly within
  // OBJPTR. OBJPTR points to this base object. BOFF indicates how SUBTYPE
  // objects might be contained within this type.  If SUBPTR is one of our
  // SUBTYPE bases, indicate virtuality. Returns not_contained for non
  // containment or private containment.
  sub_kind find_public_subobj (int boff, const type_info &subtype,
                               void *objptr, void *subptr) const
  {
    if (boff >= 0)
      return ((char *)subptr - (char *)objptr) == boff
              ? contained_public : not_contained;
    if (boff == -2)
      return not_contained;
    return do_find_public_subobj (boff, subtype, objptr, subptr);
  }
  
  public:
  // Helper for find_subobj. BOFF indicates how SUBTYPE bases are inherited by
  // the type started from -- which is not necessarily the current type.
  // OBJPTR points to the current base.
  virtual sub_kind do_find_public_subobj (int boff, const type_info &subtype,
                                          void *objptr, void *subptr) const;
};

// type_info for a class with one public, nonvirtual base class.

class __si_type_info : public __user_type_info {
  const __user_type_info &base;

public:
  __si_type_info (const char *n, const __user_type_info &b)
    : __user_type_info (n), base (b) { }

  private:
  virtual bool do_upcast (sub_kind access_path,
                          const type_info &target, void *obj,
                          upcast_result &__restrict result) const;
  virtual bool do_dyncast (int boff, sub_kind access_path,
                           const type_info &target, void *obj,
                           const type_info &subtype, void *subptr,
                           dyncast_result &__restrict result) const;
  virtual sub_kind do_find_public_subobj (int boff, const type_info &subtype,
                                          void *objptr, void *subptr) const;
};

// type_info for a general class.

#if defined(__GXX_ABI_VERSION) && __GXX_ABI_VERSION >= 100
typedef int USItype __attribute__ ((mode (SI)));
#else
typedef unsigned int USItype	__attribute__ ((mode (SI)));
#endif

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

  public:
  virtual bool do_upcast (sub_kind access_path,
                          const type_info &target, void *obj,
                          upcast_result &__restrict result) const;
  virtual bool do_dyncast (int boff, sub_kind access_path,
                           const type_info &target, void *obj,
                           const type_info &subtype, void *subptr,
                           dyncast_result &__restrict result) const;
  virtual sub_kind do_find_public_subobj (int boff, const type_info &subtype,
                                          void *objptr, void *subptr) const;
};
