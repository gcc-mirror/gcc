// Build don't link: 
// GROUPS passed conversions
// cvt file
// Message-Id: <93Aug2.163542pdt.26892@franklin.parc.xerox.com>
// From: Jesse Hull <jhull@parc.xerox.com>
// Subject: typedef bug
// Date:   Mon, 2 Aug 1993 16:35:28 PDT

typedef int A[10];

int main()
{
   int* a1 = new A;
};
