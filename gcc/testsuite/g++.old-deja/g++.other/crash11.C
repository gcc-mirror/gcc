// { dg-do assemble  }
// Origin: Alfred Minarik <a8601248@unet.univie.ac.at>

template <typename T>
struct allocator
{
  typedef int size_type;
};

template <typename T>
struct string
{
  typedef typename allocator<T>::size_type size_type;

  static size_type size;

  size_type
  max_size() const { return size; }
};

template struct string <char>;
