// { dg-do compile }

// Origin: gcc-bug@vogtner.de

// PR c++/14106: ICE with typeof of function template.

template<class T>
void j (T i)
{
}

template<typename T>
void instanciate () {
   static void (*fp) (T) = j;
   __typeof__ (j) *p;	// { dg-error "unknown|invalid" }
}
template void instanciate<float>();
