// Build don't link:
template <int object_size>
class _fixed_size_allocator
{
  private:

    struct      something { };
    static something *  asdf;

  public:

    static void         delete_object ();
};


template <class T>
class object_allocator
{
  private:

    typedef     _fixed_size_allocator<sizeof (T)>               allocator;

  public:

    static void         deallocate (T * p)
    {
        allocator::delete_object (reinterpret_cast<void *> (p));
    }
};

