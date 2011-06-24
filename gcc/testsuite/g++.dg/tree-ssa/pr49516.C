/* { dg-do run } */
/* { dg-options "-O2" } */

extern "C" void abort (void);

typedef int int32;
typedef unsigned int uint32;
typedef unsigned long long uint64;
typedef short int16;

class Tp {
 public:
  Tp(int, const int segment, const int index) __attribute__((noinline));

  inline bool operator==(const Tp& other) const;
  inline bool operator!=(const Tp& other) const;
  int GetType() const { return type_; }
  int GetSegment() const { return segment_; }
  int GetIndex() const { return index_; }
 private:
  inline static bool IsValidSegment(const int segment);
  static const int kSegmentBits = 28;
  static const int kTypeBits = 4;
  static const int kMaxSegment = (1 << kSegmentBits) - 1;

  union {

    struct {
      int32 index_;
      uint32 segment_ : kSegmentBits;
      uint32 type_ : kTypeBits;
    };
    struct {
      int32 dummy_;
      uint32 type_and_segment_;
    };
    uint64 value_;
  };
};

Tp::Tp(int t, const int segment, const int index)
 : index_(index), segment_(segment), type_(t) {}

inline bool Tp::operator==(const Tp& other) const {
  return value_ == other.value_;
}
inline bool Tp::operator!=(const Tp& other) const {
  return value_ != other.value_;
}

class Range {
 public:
  inline Range(const Tp& position, const int count) __attribute__((always_inline));
  inline Tp GetBeginTokenPosition() const;
  inline Tp GetEndTokenPosition() const;
 private:
  Tp position_;
  int count_;
  int16 begin_index_;
  int16 end_index_;
};

inline Range::Range(const Tp& position,
                    const int count)
    : position_(position), count_(count), begin_index_(0), end_index_(0)
    { }

inline Tp Range::GetBeginTokenPosition() const {
  return position_;
}
inline Tp Range::GetEndTokenPosition() const {
  return Tp(position_.GetType(), position_.GetSegment(),
            position_.GetIndex() + count_);
}

int main ()
{
  Range range(Tp(0, 0, 3), 0);
  if (!(range.GetBeginTokenPosition() == Tp(0, 0, 3)))
    abort ();

  if (!(range.GetEndTokenPosition() == Tp(0, 0, 3)))
    abort();

  return 0;
}
