// { dg-do compile }

// Origin: Francesco Monica <fmonica@ce.unipr.it>

// PR c++/13262: Access checking during instantiation of static data
// member.

template <typename T> class Aclass {
  private:
    Aclass() {}
    static Aclass instance;
};

template <typename T> Aclass<T> Aclass<T>::instance;

template class Aclass<int>;
