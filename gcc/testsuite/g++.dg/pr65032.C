// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-Os -std=c++11 -fPIC -fstack-protector-strong -fomit-frame-pointer" }

#pragma GCC visibility push(hidden)
#pragma GCC visibility push(default)
extern "C" {
  typedef int int64_t __attribute__ ((__mode__ (__DI__)));
}
enum class nsresult;
#pragma GCC visibility pop
class A
{
  float mRawPtr;

 public:
  A (float *);
};
class B
{
 public:
  B (int64_t, int, int);
};
typedef struct
{
  int channels;
} vorbis_info;
template <typename _Key> class C
{
 public:
  typedef int size_type;
  size_type erase (_Key &);
};

template <typename _Key> class D
{
 public:
  typedef _Key key_type;
  typedef C<key_type> _Rep_type;
  _Rep_type _M_t;
  typename _Rep_type::size_type
    erase (key_type p1)
  {
    return _M_t.erase (p1);
  }
};

class F
{
 public:
  vorbis_info mInfo;
  D<int *> mVorbisPacketSamples;
  void ValidateVorbisPacketSamples (int *);
  int64_t Time (int64_t);
};
class G
{
  nsresult DecodeVorbis (int *);
  F *mVorbisState;
  int64_t mDecodedAudioFrames;
};
int fn1 (float ***);
void fn2 (int);
void
F::ValidateVorbisPacketSamples (int *p1)
{
  mVorbisPacketSamples.erase (p1);
}

nsresult
G::DecodeVorbis (int *p1)
{
  float **a;
  int b;
  long long c;
  while ((b = fn1 (&a)))
    {
      mVorbisState->ValidateVorbisPacketSamples (p1);
      A (new float);
      for (; mVorbisState->mInfo.channels;)
	{
	}
      int64_t d = mVorbisState->Time (c - b);
      (B (d, b, mVorbisState->mInfo.channels));
      mDecodedAudioFrames -= b;
      fn2 (b);
    }
}
