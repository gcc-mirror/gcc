// PR c++/112869
// { dg-do compile }

void min(long, long);
template <class T> void Binaryread(int &, T, unsigned long);
template <> void Binaryread(int &, float, unsigned long bytecount) {
  min(bytecount, sizeof(int));
}
