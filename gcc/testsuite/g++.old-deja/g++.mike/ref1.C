int count;

struct base {
  base () { ++count; }
  ~base () { --count; }
  base(const base&o) { ++count; }
};

base base_returning_function ();

const base& base_ref = base_returning_function ();

int main () {
  if (count != 1)
    return 1;
}

base base_returning_function () {
  base local_base_object;
  return local_base_object;
}
