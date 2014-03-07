// PR c++/50531
// { dg-do compile { target c++11 } }

template <typename T>
class DataFilter
{
 public:
  inline virtual ~DataFilter();
};

template<typename T>
inline DataFilter<T>::~DataFilter() = default;

class ARCalculator : public DataFilter<ARCalculator>
{
 public:
  virtual void dataStart(int, int);
};

void ARCalculator::dataStart(int, int)
{}
