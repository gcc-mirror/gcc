// RTTI support internals for -*- C++ -*-
// Copyright (C) 1994, 1995, 1996, 1998, 1999, 2000 Free Software Foundation

#include "typeinfo"

// Class declarations shared between the typeinfo implementation files.

#if !defined(__GXX_ABI_VERSION) || __GXX_ABI_VERSION < 100
// original (old) abi

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

// Kludge, kludge, kludge.
#if BITS_PER_UNIT == 8
typedef int myint32 __attribute__ ((mode (SI)));
#elif BITS_PER_UNIT == 16
typedef int myint32 __attribute__ ((mode (HI)));
#elif BITS_PER_UNIT == 32
typedef int myint32 __attribute__ ((mode (QI)));
#endif

struct __class_type_info : public __user_type_info {
  enum access { PUBLIC = 1, PROTECTED = 2, PRIVATE = 3 };

  struct base_info {
    const __user_type_info *base;
    myint32 offset: 29;
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
#else
// new abi
#include "stddef.h"

namespace std {

class __class_type_info;

// helper class for __vmi_class_type
struct __base_class_info {
  const __class_type_info *type;  // base class type
  ptrdiff_t offset;               // offset to the sub object
  int vmi_flags;                  // about the base

// implementation specific parts
  enum vmi_masks {
    virtual_mask = 0x1,
    public_mask = 0x2,
    hwm_bit = 2
  };
  
public:
  bool is_virtual_p () const
    { return vmi_flags & virtual_mask; }
  bool is_public_p () const
    { return vmi_flags & public_mask; }
};

// type information for a class
class __class_type_info : public type_info {
protected:
  virtual ~__class_type_info ();
public:
  int details;      // details about the class heirarchy

// implementation specific parts
  enum detail_masks {
    multiple_base_mask = 0x1,   // multiple inheritance of the same base type
    polymorphic_mask = 0x2,     // is a polymorphic type
    virtual_base_mask = 0x4,    // has virtual bases (direct or indirect)
    private_base_mask = 0x8     // has private bases (direct or indirect)
  };

public:
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
    
    contained_virtual_mask = __base_class_info::virtual_mask, // via a virtual path
    contained_public_mask = __base_class_info::public_mask,   // via a public path
    contained_mask = 1 << __base_class_info::hwm_bit,         // contained within us
    
    contained_private = contained_mask,
    contained_public = contained_mask | contained_public_mask
  };

public:  
  struct upcast_result
  {
    const void *dst_ptr;        // pointer to caught object
    sub_kind whole2dst;         // path from most derived object to target
    int src_details;            // hints about the source type
    const __class_type_info *base_type; // where we found the target,
                                // if in vbase the __class_type_info of vbase
                                // if a non-virtual base then 1
                                // else NULL
    public:
    upcast_result (int d)
      :dst_ptr (NULL), whole2dst (unknown), src_details (d), base_type (NULL)
      {}
  };

public:
  // dyncast_result is used to hold information during traversal of a class
  // heirarchy when dynamic casting.
  struct dyncast_result
  {
    const void *dst_ptr;        // pointer to target object or NULL
    sub_kind whole2dst;         // path from most derived object to target
    sub_kind whole2src;         // path from most derived object to sub object
    sub_kind dst2src;           // path from target to sub object
    
    public:
    dyncast_result ()
      :dst_ptr (NULL), whole2dst (unknown),
       whole2src (unknown), dst2src (unknown)
      {}
  };

public:
  explicit __class_type_info (const char *n,
                              int details_)
    : type_info (n), details (details_)
    { }

protected:
  virtual bool do_upcast (const __class_type_info *dst_type, void **obj_ptr) const;

protected:
  virtual bool do_catch (const type_info *thr_type, void **thr_obj,
                         unsigned outer) const;


public:
  // Helper for upcast. See if DST is us, or one of our bases. ACCESS_PATH
  // gives the access from the start object. Return TRUE if we know the upcast
  // fails.
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;

public:
  // Indicate whether SRC_PTR of type SRC_TYPE is contained publicly within
  // OBJ_PTR. OBJ_PTR points to a base object of our type, which is the
  // destination type. SRC2DST indicates how SRC objects might be contained
  // within this type.  If SRC_PTR is one of our SRC_TYPE bases, indicate the
  // virtuality. Returns not_contained for non containment or private
  // containment.
  inline sub_kind find_public_src (ptrdiff_t src2dst, const void *obj_ptr,
                                   const __class_type_info *src_type,
                                   const void *src_ptr) const;

public:
  // dynamic cast helper. ACCESS_PATH gives the access from the most derived
  // object to this base. DST_TYPE indicates the desired type we want. OBJ_PTR
  // points to a base of our type within the complete object. SRC_TYPE
  // indicates the static type started from and SRC_PTR points to that base
  // within the most derived object. Fill in RESULT with what we find. Return
  // true if we have located an ambiguous match.
  virtual bool do_dyncast (ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
public:
  // Helper for find_public_subobj. SRC2DST indicates how SRC_TYPE bases are
  // inherited by the type started from -- which is not necessarily the
  // current type. The current type will be a base of the destination type.
  // OBJ_PTR points to the current base.
  virtual sub_kind do_find_public_src (ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *src_ptr) const;
};

// type information for a class with a single non-virtual base
class __si_class_type_info : public __class_type_info {
protected:
  virtual ~__si_class_type_info ();
protected:
  const __class_type_info *base;    // base type

public:
  explicit __si_class_type_info (const char *n,
                                 int details_,
                                 const __class_type_info *base_)
    : __class_type_info (n, details_), base (base_)
    { }

// implementation specific parts
protected:
  virtual bool do_dyncast (ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
  virtual sub_kind do_find_public_src (ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *sub_ptr) const;
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;
};

// type information for a class with multiple and/or virtual bases
class __vmi_class_type_info : public __class_type_info {
protected:
  virtual ~__vmi_class_type_info ();
protected:
  int n_bases;      // number of direct bases
  __base_class_info base_list[1]; // array of bases
  // The array of bases uses the trailing array struct hack
  // so this class is not constructable with a normal constructor. It is
  // internally generated by the compiler.

public:
  explicit __vmi_class_type_info (const char *n,
                                  int details_)
    : __class_type_info (n, details_), n_bases (0)
    { }

// implementation specific parts
protected:
  virtual bool do_dyncast (ptrdiff_t src2dst, sub_kind access_path,
                           const __class_type_info *dst_type, const void *obj_ptr,
                           const __class_type_info *src_type, const void *src_ptr,
                           dyncast_result &result) const;
  virtual sub_kind do_find_public_src (ptrdiff_t src2dst, const void *obj_ptr,
                                       const __class_type_info *src_type,
                                       const void *src_ptr) const;
  virtual bool do_upcast (sub_kind access_path,
                          const __class_type_info *dst, const void *obj,
                          upcast_result &__restrict result) const;
};

// dynamic cast runtime
void *__dynamic_cast (const void *src_ptr,    // object started from
                      const __class_type_info *src_type, // static type of object
                      const __class_type_info *dst_type, // desired target type
                      ptrdiff_t src2dst); // how src and dst are related

    // src2dst has the following possible values
    // >= 0: src_type is a unique public non-virtual base of dst_type
    //       dst_ptr + src2dst == src_ptr
    // -1: unspecified relationship
    // -2: src_type is not a public base of dst_type
    // -3: src_type is a multiple public non-virtual base of dst_type

}; // namespace std

#endif
