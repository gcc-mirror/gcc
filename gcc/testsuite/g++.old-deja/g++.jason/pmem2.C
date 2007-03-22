// { dg-do run  }
template <class Called>
class aCallback
{
public:
  aCallback(Called& obj, int (Called::*met)());

  int callback();
  
protected:

private:
  Called&       object;
  
  int (Called::*method)();
  
};

template <class Called>
aCallback<Called>::aCallback(Called& obj,
                             int (Called::*met)()) :
object(obj),
method(met)
{}

template <class Called>
int aCallback<Called>::callback()
{
  return (object.*method)();
}

struct myStruct
{
  int action() {return 0;}
};

int main()
{
  myStruct toto;

  aCallback<myStruct>   cb(toto, &myStruct::action);

  return cb.callback();
}
