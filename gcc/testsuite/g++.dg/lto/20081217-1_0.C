class base1
{
 public:
 base1 () {}
 virtual ~base1 () {}
};

class base2
{
 public:
 base2 () {}
 virtual ~base2 () {}
};

class mi_class : public base1, base2
{
 public:
 mi_class () {}
 ~mi_class () {}
};

mi_class dummy;

int
main ()
{
 return 0;
}
