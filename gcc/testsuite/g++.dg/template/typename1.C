// Origin: Volker Reichelt <reichelt@igpm.rwth-aachen.de>
// { dg-do compile }

template <class T>
class B : virtual T::A
{
	typedef int INT;
	INT i;
};
