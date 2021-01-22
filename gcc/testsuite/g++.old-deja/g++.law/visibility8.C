// { dg-do assemble  }
// GROUPS passed visibility
// visibility file
// From: roland@jts.com (Roland Knight )
// Date:     Thu, 29 Apr 1993 16:17:00 -0400
// Subject:  gcc 2.3.3 bug
// Message-ID: <m0nof3E-0021ifC@jts.com
class t1 {
protected:
    int a; 
};


class t2 : private t1 // { dg-message "" } protected
{ 
public:
    int b;
};


class t3 : public t2 {
public:
    int ttt();
};


int t3::ttt() { return a; }// { dg-error "" } .*
