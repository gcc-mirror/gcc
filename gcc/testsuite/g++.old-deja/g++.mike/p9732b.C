// prms-id: 9732

int count;
int bail = 0;

extern "C" void _exit (int);

struct base {
  base () { ++count; }
  ~base () { --count; }
  base(const base&o) { ++count; }
};

class D {
public:
  ~D() {
    if (bail++)
      {
	// On some Linux boxes, we run the dtor for d twice,
	// once before exit, and once after!
	abort ();
      }
    else
      {
	if (count != 0)
	  _exit (1);
	_exit (0);
      }
  }
} d;

base base_object;

base base_returning_function ();

const base& base_ref = base_returning_function ();

int main () {
}

base base_returning_function () {
  base local_base_object;
  return local_base_object;
}
