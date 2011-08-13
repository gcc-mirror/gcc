 // { dg-do run }

struct Spec

{
 virtual int id () const = 0;
};
class D1_1_Spec :  public virtual Spec { };
class D1_2_Spec :  public virtual Spec { };
class D1_3_Spec :  public virtual Spec { };
class D2_1_Spec : public D1_1_Spec, public D1_2_Spec { };
class D2_Spec : public virtual D2_1_Spec, public virtual D1_3_Spec { };

struct D3_Spec : public D2_Spec
{
 virtual int id () const { return 3; }

};

__attribute__((noinline)) void foo(D3_Spec* spec)
{
  spec->id();
}

int main()
{
 D3_Spec spec;
 foo(&spec);
 return 0;
}
