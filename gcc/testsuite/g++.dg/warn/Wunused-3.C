// { dg-do compile }
// { dg-options "-Wunused -O" }

void do_cleanups();

class Cleanup {
public:
    ~Cleanup() { do_cleanups();}
};

static Cleanup dummy;
