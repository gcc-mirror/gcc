// Build don't link: 
// GROUPS passed overloading
struct temp_string {
  temp_string (const unsigned char);
};
  
class String {
 public:
  String& operator = (temp_string);
  String& operator = (const String&);
}; 
class S {
 public:
  operator temp_string & () const;
}; 

S lbuf;

static void e_r ()
{
  String a;
  a = lbuf;
 return;
}
