// { dg-do run  }
// GROUPS passed initialization
// init file
// Message-Id: <9306280834.AA20921@slsvitt.us-es.sel.de>
// From: David Binderman 3841 <dcb@us-es.sel.de>
// Subject: Page 289 of the ARM
// Date: Mon, 28 Jun 93 10:34:37 +0200

struct T {
  int m;

  T( int g):m(g){}
};

// T s1[2][2] = {1,2,3,4};

int main() {
	T s1[2][2] = {1,2,3,4};
	return 0;
}
