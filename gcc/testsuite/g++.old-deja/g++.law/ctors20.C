// Build don't link: 
// GROUPS passed constructors
// ctor file
// Message-Id: <9311011758.AA25157@thneed.cs.duke.edu>
// From: Vivek Khera <khera@cs.duke.edu>
// Subject: g++ 2.5.0 fails to automatically generate default initializer
// Date: Mon, 01 Nov 1993 12:58:34 -0500

class String
{
  private:
    char a[100];
    int len;
  public:
    String();
};

String::String()
{
    len = 0;
}


struct List
{
    String item[100];
    int num_items;
//    List();                   // uncomment this line to let compile work
};

int
main(int argc, char **argv)
{
    List a;
}
