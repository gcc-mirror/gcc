// { dg-do assemble  }
// GROUPS passed parsing
// parsing folder
// From: Glenn Engel <glenne@lsid.hp.com>
// Date:     Fri, 29 Jan 93 18:42:03 PST
// Subject:  Parse Error
// Message-ID: <9301300242.AA15550@hplslk.lsid.hp.com>

int test1(void (**roc)(int,int)); // parse error
int test2(int id,void (**orc)(int,int)); // works
