// PR c++/14116
// Any use of typeof in a templete was causing an ICE.
// { dg-options "" }

struct vector { typedef int iterator; };
vector read_queue;
template <class T> void f(){
    typedef typeof (read_queue) read_queue_t;
    read_queue_t::iterator it;
}


