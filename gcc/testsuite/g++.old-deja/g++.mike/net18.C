// { dg-do assemble  }

class ClassA {
public:
  typedef ClassA& (*PMFV)(const char*);
  static PMFV setMapper(PMFV);
  static PMFV _mapper;
};

class ClassB {
public:
  typedef ClassB& (*PMFV)(const char*);
};

ClassA::PMFV ClassA::setMapper(ClassA::PMFV newFunc)
{
  PMFV oldFunc = _mapper;
  _mapper = newFunc;

  return oldFunc;
}
