/* { dg-do run } */
/* { dg-options "-O2 -std=c++11" } */

void thrower[[gnu::noinline]]() {
    throw 1;
}

inline void fatal() noexcept {thrower();}
inline void notFatal() {thrower();}

void func(bool callFatal) {
    if (callFatal) {
        fatal();
    } else { 
        notFatal();
    }
}

int main(int argc, const char* argv[]) {
    try {
        bool callFatal = argc > 1;
        func(callFatal);
    } catch (...) {
    }
}
