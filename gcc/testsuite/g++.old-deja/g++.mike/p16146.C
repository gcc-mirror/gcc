// { dg-do run  }
// prms-id: 16146

extern "C" int printf (const char *, ...);

class myFoundation {
protected:
  myFoundation () { count = 0; }
  virtual ~myFoundation () {}

public:
  void addRef () { ++count; }
  void removeRef () { if (count > 0) --count; }

private:
  long count;
};


class firstIntermediate :virtual public myFoundation {
public:
  firstIntermediate () {}
  ~firstIntermediate () {}

  void bar () { printf ("Bar\n"); }
};


class firstBase	:  public firstIntermediate {
public:
  firstBase () {}
  ~firstBase () {}

  virtual void g () {}
};


class secondIntermediate : virtual public myFoundation {
public:
  secondIntermediate () {}
  ~secondIntermediate () {}

  virtual void h () {}
};


class secondBase : public secondIntermediate {
public:
  secondBase () {}
  ~secondBase () {}

  virtual void h () {}
};


class typeInterface : virtual public firstBase {
public:
  typeInterface () {}
  ~typeInterface () {}

  virtual void i () {}
};

class classServices : virtual public firstBase,
		      public secondBase {
public:
  classServices () {}
  ~classServices () {}

  virtual void j () {}
};

class classImplementation : public typeInterface,
			    public classServices {
public:
  classImplementation () {}
  ~classImplementation () {}

  void g () {}
  void h () {}
  void i () {}
  void j () {}
};

int main () {
  firstBase* fbp = new classImplementation;
  classImplementation* cip = dynamic_cast <classImplementation*> (fbp);
  cip->addRef();
  myFoundation* mfp = cip;
}
