// Build don't link:
// Origin: Alfred Minarik <a8601248@unet.univie.ac.at>
// Special g++ Options: 

template<typename _CharT>
struct basic_filebuf
{
  virtual void 
  underflow()
    {
      int __size = 5;
      char __conv_buf[__size];
    }
};

template class basic_filebuf<char>;
