// Build don't run:

template<class T,class T1>
int connect_to_method(T* receiver,
                      int (T1::*method)()) 
{ 
  return (receiver->*method)();
}

class Gtk_Container
{
public:
  int remove_callback() { return 1; }
  void remove_callback(int);
  int f();
};

int Gtk_Container::f() 
{
  return connect_to_method(this, &Gtk_Container::remove_callback);
}

int main()
{
  Gtk_Container gc;
  if (gc.f () != 1)
    return 1;
}
