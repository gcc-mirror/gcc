// Build don't link:
// Origin: "Chen, Wen-Ke" <chwk@cs.arizona.edu>

template <class T>
bool operator!=(const T&, const T&);

enum MsgType {
  MSG_DATA
};

class C {
public:
  MsgType mType : 8;
};

int main(void)
{
  extern C& c;

  c.mType = MSG_DATA; 
  if (c.mType != MSG_DATA)
    return -1;

  return 0;
}
