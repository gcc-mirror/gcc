// Copyright (C) 2002 Free Software Foundation
// Origin: C++/717
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net
// { dg-do compile }

class _eAny
{
public:
   _eAny(){}
};

template <class X>
class _eSeq
{
public:
   _eSeq(const X thing){}
   int _oHash() {return 0;}
   _eSeq<X > _oPlusPlus(const _eSeq<X > other) const
   {
      return *this;
   }
   _eSeq<X > take(const _eSeq<X > other) const
   {
      return *this;
   }
};


template <class X>
class _eHndl
{
public:
   _eHndl(const _eAny *obj){}
};

class VarInstances : public _eAny
{
public:
   VarInstances() : _eAny() {}
};

void testFunc(const VarInstances *testInstance)
{
   const _eSeq<_eHndl<VarInstances> > temp1 =
      _eSeq<_eHndl<VarInstances> >(_eHndl<VarInstances>(testInstance));

   if((_eSeq<_eHndl<VarInstances>
       >(_eHndl<VarInstances>(testInstance))._oPlusPlus(temp1)._oHash() ==
       7))
      {
         return;
      }
}

int main(int argc, char** argv)
{
   testFunc(new VarInstances());
}
