// PR c++/14035
// { dg-do run }

extern "C" void abort();

struct Blob {
  int x, y;
  Blob() { }
  Blob(const Blob &b) { abort (); }
};
struct Blobby : public Blob { };

struct Wooly {
  operator const Blobby & ()
  {
    return myBlobby;
  }
  Blobby myBlobby;
};

void catcher(const Blob &blo)
{ }

int main()
{
  Wooly wooly;
  catcher((const Blob &)wooly);
}
