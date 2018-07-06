namespace __sanitizer {

enum FileAccessMode {
  RdOnly,
  WrOnly,
  RdWr
};

int OpenFile(const char *filename, int mode,
	     int *errno_p);
}

using namespace __sanitizer;

namespace __sancov {

static int OpenFile(const char* path) {
  int err;
  int fd = OpenFile(path, WrOnly, &err);
  return fd;
}
}
