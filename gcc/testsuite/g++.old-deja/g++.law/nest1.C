// { dg-do assemble  }
// GROUPS passed nest
// nest file
// From: gs4t@virginia.edu (Gnanasekaran  Swaminathan)
// Date:     Wed, 30 Dec 1992 20:38:07 GMT
// Subject:  Local type names bug in g++ 2.3.3
// Message-ID: <1992Dec30.203807.17504@murdoch.acc.Virginia.EDU>

typedef char* T; // { dg-error "" } previous declaration

struct Y {
    T a;
    typedef long T; // error. See ARM p189-191 for details// { dg-error "" } 
    T b;
};
