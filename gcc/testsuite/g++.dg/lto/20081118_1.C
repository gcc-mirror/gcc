class object {
  virtual ~object() {}
};

class bar : public object
{
  static bar *method(void);
};

class quxx : public bar
{
 public:
  static void method();
};

bar*
bar::method (void)
{
 quxx::method();
}
