/* { dg-additional-options "-std=c++20 -mlra -fpic -w " }  */
/* { dg-do compile }  */

typedef unsigned int size_t;

extern "C++"
{

namespace std __attribute__ ((__visibility__ ("default")))
{

template<typename _Tp, _Tp __v> struct integral_constant
{
  static constexpr _Tp value = __v;
  using value_type = _Tp;
  constexpr operator value_type() const noexcept { return value; }

  constexpr value_type operator()() const noexcept { }
 };

template<bool __v> using __bool_constant = integral_constant<bool, __v>;
using true_type = __bool_constant<true>;
using false_type = __bool_constant<false>;

template<bool, typename _Tp = void> struct enable_if { };

template<typename _Tp> struct enable_if<true, _Tp> {  using type = _Tp; };

template<bool _Cond, typename _Tp = void> using __enable_if_t = typename enable_if<_Cond, _Tp>::type;
template<bool> struct __conditional {  template<typename _Tp, typename> using type = _Tp;  };
template<bool _Cond, typename _If, typename _Else> using __conditional_t = typename __conditional<_Cond>::template type<_If, _Else>;
template <typename _Type> struct __type_identity {  };

namespace __detail
{
template<typename _Tp, typename...> using __first_t = _Tp;
template<typename... _Bn> auto __and_fn(int) -> __first_t<true_type, __enable_if_t<bool(_Bn::value)>...>;
}

template<typename... _Bn> struct __and_ : decltype(__detail::__and_fn<_Bn...>(0)) { };
template<typename _Pp> struct __not_ : __bool_constant<!bool(_Pp::value)> { };

template <typename _Tp, size_t = sizeof(_Tp)> constexpr true_type __is_complete_or_unbounded(__type_identity<_Tp>) {  return { }; }
template<typename _Tp> struct is_pointer : public __bool_constant<__is_pointer(_Tp)> { };
template<typename _Tp> struct is_empty : public __bool_constant<__is_empty(_Tp)> { };
template<typename _Tp, typename... _Args> using __is_constructible_impl = __bool_constant<__is_constructible(_Tp, _Args...)>;

template<typename _Tp> struct is_default_constructible : public __is_constructible_impl<_Tp> {  };
template<typename _Tp> using __add_lval_ref_t = __add_lvalue_reference(_Tp);
template<typename _Tp> using __add_rval_ref_t = __add_rvalue_reference(_Tp);
template<typename _Tp> struct is_move_constructible : public __is_constructible_impl<_Tp, __add_rval_ref_t<_Tp>>
{
 static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp>{ }
), "template argument must be a complete class or an unbounded array");
};

template<typename _Tp, typename _Up> using __is_assignable_impl = __bool_constant<__is_assignable(_Tp, _Up)>;
template<typename _Tp> struct is_move_assignable : public __is_assignable_impl<__add_lval_ref_t<_Tp>, __add_rval_ref_t<_Tp>>
{
  static_assert(std::__is_complete_or_unbounded(__type_identity<_Tp> {}
), "template argument must be a complete class or an unbounded array");

};

template<size_t __i, typename _Tp> struct tuple_element;
template<size_t __i, typename _Tp> using __tuple_element_t = typename tuple_element<__i, _Tp>::type;
template<size_t _Np, typename... _Types> struct _Nth_type
{
  using type = __type_pack_element<_Np, _Types...>;
};

template<typename...> class tuple;
template<size_t __i, typename... _Elements> constexpr const __tuple_element_t<__i, tuple<_Elements...>>& get(const tuple<_Elements...>& __t) noexcept;
}
}

inline constexpr float SK_FloatSqrt2 = 1.41421356f;

struct SkPoint
{
  float fY;
};

extern void sk_free(void*);
enum
{
 SK_MALLOC_ZERO_INITIALIZE = 1 << 0, SK_MALLOC_THROW = 1 << 1,
};

extern void* sk_malloc_flags(size_t size, unsigned flags);

static inline void* sk_calloc_throw(size_t size)
{
  return sk_malloc_flags(size, SK_MALLOC_THROW | SK_MALLOC_ZERO_INITIALIZE);
}

typedef struct
{
   union
   {
   } __value;

} __mbstate_t;

namespace std __attribute__ ((__visibility__ ("default")))
{
template<typename _Tp> struct __is_empty_non_tuple : is_empty<_Tp> { };

template<typename _Tp> using __empty_not_final = __conditional_t<__is_final(_Tp), false_type, __is_empty_non_tuple<_Tp>>;
template<size_t _Idx, typename _Head, bool = __empty_not_final<_Head>::value> struct _Head_base;
template<size_t _Idx, typename _Head> struct _Head_base<_Idx, _Head, false> { };
template<size_t _Idx, typename... _Elements> struct _Tuple_impl;
template<size_t _Idx, typename _Head, typename... _Tail> struct _Tuple_impl<_Idx, _Head, _Tail...> : public _Tuple_impl<_Idx + 1, _Tail...>, private _Head_base<_Idx, _Head> { };
template<size_t _Idx, typename _Head> struct _Tuple_impl<_Idx, _Head> : private _Head_base<_Idx, _Head> { };
template<typename... _Elements> class tuple : public _Tuple_impl<0, _Elements...> { };
template<size_t __i, typename... _Types> struct tuple_element<__i, tuple<_Types...>>
{
  using type = typename _Nth_type<__i, _Types...>::type;
};

template<typename _Tp> struct default_delete
{
};

template <typename _Tp, typename _Dp> class __uniq_ptr_impl
{
  template <typename _Up, typename _Ep, typename = void> struct _Ptr {  using type = _Up*;  };

public:
   using _DeleterConstraint = enable_if< __and_<__not_<is_pointer<_Dp>>, is_default_constructible<_Dp>>::value>;
   using pointer = typename _Ptr<_Tp, _Dp>::type;
   constexpr __uniq_ptr_impl(pointer __p) : _M_t() { }
   constexpr pointer _M_ptr() const noexcept { return std::get<0>(_M_t); }
private:
  tuple<pointer, _Dp> _M_t;
};

template <typename _Tp, typename _Dp, bool = is_move_constructible<_Dp>::value, bool = is_move_assignable<_Dp>::value> struct __uniq_ptr_data : __uniq_ptr_impl<_Tp, _Dp>{ };

template <typename _Tp, typename _Dp = default_delete<_Tp>> class unique_ptr
{
  template <typename _Up> using _DeleterConstraint = typename __uniq_ptr_impl<_Tp, _Up>::_DeleterConstraint::type;
  __uniq_ptr_data<_Tp, _Dp> _M_t;
public:
  using pointer = typename __uniq_ptr_impl<_Tp, _Dp>::pointer;
  template<typename _Del = _Dp, typename = _DeleterConstraint<_Del>> constexpr explicit unique_ptr(pointer __p) noexcept : _M_t(__p) { }

   constexpr pointer get() const noexcept { return _M_t._M_ptr(); }
};

}

template <typename T, T* P> struct SkOverloadedFunctionObject
{
};

namespace skia_private
{
  using UniqueVoidPtr = std::unique_ptr<void, SkOverloadedFunctionObject<void(void*), sk_free>>;
}

class SkNoncopyable
{
};

template <size_t kSizeRequested> class SkAutoSMalloc : SkNoncopyable
{
  public: SkAutoSMalloc() { }

  explicit SkAutoSMalloc(size_t size) { }

  void* get() const { return fPtr; }

  void* fPtr;
};

         class SkPointPriv {
       public: enum Side {
    kLeft_Side = -1, kOn_Side = 0, kRight_Side = 1, };
       static bool SetLengthFast(SkPoint* pt, float length);
       };
         using namespace skia_private;
         struct DFData {
       float fAlpha;
       SkPoint fDistVector;
       };
         enum NeighborFlags {
       kLeft_NeighborFlag = 0x01, kRight_NeighborFlag = 0x02, kTopLeft_NeighborFlag = 0x04, kTop_NeighborFlag = 0x08, kTopRight_NeighborFlag = 0x10, kBottomLeft_NeighborFlag = 0x20, kBottom_NeighborFlag = 0x40, kBottomRight_NeighborFlag = 0x80, kAll_NeighborFlags = 0xff, kNeighborFlagCount = 8 };
         static void init_glyph_data(DFData* data, unsigned char* edges, const unsigned char* image, int dataWidth, int dataHeight, int imageWidth, int imageHeight, int pad) {
       for (int j = 0;
       j < imageHeight;
       ++j) {
    for (int i = 0;
    i < imageWidth;
    ++i) {
 if (255 == *image) {
 }
 }
    }
       }

static void init_distances(DFData* data, unsigned char* edges, int width, int height)
{
  DFData* currData = data;
  DFData* prevData = data - width;
  DFData* nextData = data + width;
  for (int j = 0; j < height; ++j)
  {
    for (int i = 0;i < width;++i)
    {
      if (*edges)
      {
        SkPoint currGrad;
        currGrad.fY = (nextData-1)->fAlpha - (prevData-1)->fAlpha + SK_FloatSqrt2*nextData->fAlpha - SK_FloatSqrt2*prevData->fAlpha + (nextData+1)->fAlpha - (prevData+1)->fAlpha;
        SkPointPriv::SetLengthFast(&currGrad, 1.0f);
        currData->fDistVector.fY = 1000.f;
      }
     ++currData;
     ++prevData;
     ++nextData;
    }
  }
}

static void B1(DFData* curr, int width)
{
}

static bool generate_distance_field_from_image(unsigned char* distanceField, const unsigned char* copyPtr, int width, int height)
{
  int pad = 4 + 1;
  int dataWidth = width + 2*pad;
  int dataHeight = height + 2*pad;
  UniqueVoidPtr storage(sk_calloc_throw(dataWidth*dataHeight*(sizeof(DFData) + 1)));
  DFData* dataPtr = (DFData*)storage.get();
  unsigned char* edgePtr = (unsigned char*)storage.get() + dataWidth*dataHeight*sizeof(DFData);
  init_distances(dataPtr, edgePtr, dataWidth, dataHeight);
  unsigned char* currEdge = edgePtr+dataWidth+1;
  for (int j = 1; j < dataHeight-1; ++j)
  {
    for (int i = 1; i < dataWidth-1; ++i)
    {
      if (!*currEdge)
      {
      }
    }
  }

  // return true;
  // ICE triggered only without return statement
}


bool SkGenerateDistanceFieldFromA8Image(unsigned char* distanceField, const unsigned char* image, int width, int height, size_t rowBytes)
{
  SkAutoSMalloc<1024> copyStorage((width+2)*(height+2)*sizeof(char));
  unsigned char* copyPtr = (unsigned char*) copyStorage.get();
  return generate_distance_field_from_image(distanceField, copyPtr, width, height);
}
