// Build don't link: 
// GROUPS passed copy-ctors
// copy file
// From: Vivek Khera <khera@cs.duke.edu>
// Date:     Mon, 15 Nov 1993 16:02:18 -0500
// Subject:  g++ 2.5.3 fails to automatically generate default initializer
// Message-ID: <9311152102.AA21248@thneed.cs.duke.edu>

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
