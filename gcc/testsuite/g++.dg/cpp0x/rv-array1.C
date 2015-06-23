// PR c++/66501
// { dg-do run { target c++11 } }

int total_size;

struct Object
{
  int size = 0;

  Object () = default;

  ~Object () {
    total_size -= size;
  }

  Object (const Object &) = delete;
  Object & operator= (const Object &) = delete;

  Object (Object && b) {
    size = b.size;
    b.size = 0;
  }

  Object & operator= (Object && b) {
    if (this != & b) {
      total_size -= size;
      size = b.size;
      b.size = 0;
    }
    return * this;
  }

  void grow () {
    size ++;
    total_size ++;
  }
};

struct Container {
  Object objects[2];
};

int main (void)
{
  Container container;

  // grow some objects in the container
  for (auto & object : container.objects)
    object.grow ();

  // now empty it
  container = Container ();

  return total_size;
}
