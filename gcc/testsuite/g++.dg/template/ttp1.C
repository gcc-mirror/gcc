// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template <template <typename T> class A >
class B : virtual A<void>
{
	typedef int INT;
	INT i;
};
