// Special g++ Options: -fexceptions
// excess errors test - XFAIL a29k-*-* sparc64-*-elf sh-*-* arm-*-pe**-*
// prms-id: 9159

static unsigned int iCounter = 0;
static unsigned int iMax;
int fail = 0;

class ca {
public:
  ca(int) {
    if (iCounter++ == iMax)
      throw (const char*)"iCounter";
  }
  virtual ~ca() {
  }
};

class cc {
public:
  cc(const ca &rca1, const ca &rca2) {
  }
  virtual ~cc() {
    fail = 1;
  }
};


int main(int argc, char **argv) {
  iMax = 1;
  try {
    cc sc(ca(1), ca(1));
  } catch (const char *pMsg) {
  }
  return fail;
}
