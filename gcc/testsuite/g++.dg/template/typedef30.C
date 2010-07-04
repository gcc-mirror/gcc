// Origin: PR c++/43206
// { dg-do compile }

template<class A> struct NumericTraits{ typedef A TInputImage;};
template<class B> class CovariantVector{};
template<class C> struct Image{ typedef C PixelType;};
template<class H, class E, class D>
class F {
  typedef H G;
  typedef
  typename NumericTraits<typename G::PixelType>::RealType
  InputRealType;
};

template<typename TInputImage,
         typename TOutputImage=Image<CovariantVector<typename NumericTraits<typename TInputImage::PixelType>::TInputImage> > >
class XXX{};

XXX<Image<float> > x;

