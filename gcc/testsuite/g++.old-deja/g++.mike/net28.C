// { dg-do assemble  }
// From reinhard@ifki50.informatik.fh-muenchen.de against gcc 2.5.0.
// shows an InterViews problem on RS6000 and i386.  Doesn't happen on
// SPARC.

// From fbrowser.c.


class test {
};


typedef int ( test    ::*   test2)(int fd);

class    ivFileBrowserImpl_IOCallback
{
 public:

 int inputReady(int fd);
  test    * _obj;
  test2   _input;
}; 


int    ivFileBrowserImpl_IOCallback  ::inputReady(int fd)
{
        return (_obj->*_input)(fd);
}
