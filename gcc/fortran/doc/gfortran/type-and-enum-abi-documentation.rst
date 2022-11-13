..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _type-and-enum-abi-documentation:

Type and enum ABI Documentation
*******************************

.. toctree::
  :maxdepth: 2


.. _caf_token_t:

caf_token_t
^^^^^^^^^^^

Typedef of type ``void *`` on the compiler side. Can be any data
type on the library side.

.. _caf_register_t:

caf_register_t
^^^^^^^^^^^^^^

Indicates which kind of coarray variable should be registered.

.. code-block:: c++

  typedef enum caf_register_t {
    CAF_REGTYPE_COARRAY_STATIC,
    CAF_REGTYPE_COARRAY_ALLOC,
    CAF_REGTYPE_LOCK_STATIC,
    CAF_REGTYPE_LOCK_ALLOC,
    CAF_REGTYPE_CRITICAL,
    CAF_REGTYPE_EVENT_STATIC,
    CAF_REGTYPE_EVENT_ALLOC,
    CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY,
    CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY
  }
  caf_register_t;

The values ``CAF_REGTYPE_COARRAY_ALLOC_REGISTER_ONLY`` and
``CAF_REGTYPE_COARRAY_ALLOC_ALLOCATE_ONLY`` are for allocatable components
in derived type coarrays only.  The first one sets up the token without
allocating memory for allocatable component.  The latter one only allocates the
memory for an allocatable component in a derived type coarray.  The token
needs to be setup previously by the REGISTER_ONLY.  This allows to have
allocatable components un-allocated on some images.  The status whether an
allocatable component is allocated on a remote image can be queried by
``_caf_is_present`` which used internally by the ``ALLOCATED``
intrinsic.

.. _caf_deregister_t:

caf_deregister_t
^^^^^^^^^^^^^^^^

.. code-block:: c++

  typedef enum caf_deregister_t {
    CAF_DEREGTYPE_COARRAY_DEREGISTER,
    CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY
  }
  caf_deregister_t;

Allows to specifiy the type of deregistration of a coarray object.  The
``CAF_DEREGTYPE_COARRAY_DEALLOCATE_ONLY`` flag is only allowed for
allocatable components in derived type coarrays.

.. _caf_reference_t:

caf_reference_t
^^^^^^^^^^^^^^^

The structure used for implementing arbitrary reference chains.
A ``CAF_REFERENCE_T`` allows to specify a component reference or any kind
of array reference of any rank supported by gfortran.  For array references all
kinds as known by the compiler/Fortran standard are supported indicated by
a ``MODE``.

.. code-block:: c++

  typedef enum caf_ref_type_t {
    /* Reference a component of a derived type, either regular one or an
       allocatable or pointer type.  For regular ones idx in caf_reference_t is
       set to -1.  */
    CAF_REF_COMPONENT,
    /* Reference an allocatable array.  */
    CAF_REF_ARRAY,
    /* Reference a non-allocatable/non-pointer array.  I.e., the coarray object
       has no array descriptor associated and the addressing is done
       completely using the ref.  */
    CAF_REF_STATIC_ARRAY
  } caf_ref_type_t;

.. code-block:: c++

  typedef enum caf_array_ref_t {
    /* No array ref.  This terminates the array ref.  */
    CAF_ARR_REF_NONE = 0,
    /* Reference array elements given by a vector.  Only for this mode
       caf_reference_t.u.a.dim[i].v is valid.  */
    CAF_ARR_REF_VECTOR,
    /* A full array ref (:).  */
    CAF_ARR_REF_FULL,
    /* Reference a range on elements given by start, end and stride.  */
    CAF_ARR_REF_RANGE,
    /* Only a single item is referenced given in the start member.  */
    CAF_ARR_REF_SINGLE,
    /* An array ref of the kind (i:), where i is an arbitrary valid index in the
       array.  The index i is given in the start member.  */
    CAF_ARR_REF_OPEN_END,
    /* An array ref of the kind (:i), where the lower bound of the array ref
       is given by the remote side.  The index i is given in the end member.  */
    CAF_ARR_REF_OPEN_START
  } caf_array_ref_t;

.. code-block:: c++

  /* References to remote components of a derived type.  */
  typedef struct caf_reference_t {
    /* A pointer to the next ref or NULL.  */
    struct caf_reference_t *next;
    /* The type of the reference.  */
    /* caf_ref_type_t, replaced by int to allow specification in fortran FE.  */
    int type;
    /* The size of an item referenced in bytes.  I.e. in an array ref this is
       the factor to advance the array pointer with to get to the next item.
       For component refs this gives just the size of the element referenced.  */
    size_t item_size;
    union {
      struct {
        /* The offset (in bytes) of the component in the derived type.
           Unused for allocatable or pointer components.  */
        ptrdiff_t offset;
        /* The offset (in bytes) to the caf_token associated with this
           component.  NULL, when not allocatable/pointer ref.  */
        ptrdiff_t caf_token_offset;
      } c;
      struct {
        /* The mode of the array ref.  See CAF_ARR_REF_*.  */
        /* caf_array_ref_t, replaced by unsigend char to allow specification in
           fortran FE.  */
       unsigned char mode[GFC_MAX_DIMENSIONS];
        /* The type of a static array.  Unset for array's with descriptors.  */
        int static_array_type;
        /* Subscript refs (s) or vector refs (v).  */
        union {
          struct {
            /* The start and end boundary of the ref and the stride.  */
            index_type start, end, stride;
          } s;
          struct {
            /* nvec entries of kind giving the elements to reference.  */
            void *vector;
            /* The number of entries in vector.  */
            size_t nvec;
            /* The integer kind used for the elements in vector.  */
            int kind;
          } v;
        } dim[GFC_MAX_DIMENSIONS];
      } a;
    } u;
  } caf_reference_t;

The references make up a single linked list of reference operations.  The
``NEXT`` member links to the next reference or NULL to indicate the end of
the chain.  Component and array refs can be arbitrarily mixed as long as they
comply to the Fortran standard.

.. note::
  The member ``STATIC_ARRAY_TYPE`` is used only when the ``TYPE`` is
  ``CAF_REF_STATIC_ARRAY``.  The member gives the type of the data referenced.
  Because no array descriptor is available for a descriptor-less array and
  type conversion still needs to take place the type is transported here.

  At the moment ``CAF_ARR_REF_VECTOR`` is not implemented in the front end for
  descriptor-less arrays.  The library caf_single has untested support for it.

.. _caf_team_t:

caf_team_t
^^^^^^^^^^

Opaque pointer to represent a team-handle.  This type is a stand-in for the
future implementation of teams.  It is about to change without further notice.