// Build don't link: 
// GROUPS passed operators
// (Message bugs/opr-del:4)
// From: jamshid@ses.com (Jamshid Afshar)
// Date:     Fri, 25 Feb 94 18:44:01 CST
// Subject:  Re: delete on "smart pointers"
// Message-ID: <9402262328.AA16321@pancake>
// 
// Who was apparently replying to kuhlins@hawk.wifo.uni-mannheim.de


template<class T> class Ptr {
public:
  Ptr(T*);
  operator T*();
};

int main() {
  Ptr<int> ip = new int(2);
  delete ip;
  operator delete(ip);
  return 0;
}
