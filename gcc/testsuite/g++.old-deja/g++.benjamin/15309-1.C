// 981203 bkoz
// g++/15309
// Build don't link: 
// Special g++ Options: -Wnon-virtual-dtor -Weffc++

class bahamian {
public:
  bahamian ();
  ~bahamian ();  
};

class miami : public bahamian
{
public:
   miami ();
   ~miami ();
}; // WARNING - // WARNING -        




