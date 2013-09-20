// PR c++/24926

class A {
  union {
    int i;       // { dg-error "private" }
  };
  union {
    int j;       // { dg-error "private" }
  }; 
  union {
    union {
      int k;     // { dg-error "private" }
    };
    union {
      union {
	int l;   // { dg-error "private" }
      };
      union {
	int m;   // { dg-error "private" }
	union {
	  int n; // { dg-error "private" }
	  int o; // { dg-error "private" }
	};
      };
    };
  };
};

int a1 = A().i;  // { dg-error "context" }
int a2 = A().j;  // { dg-error "context" }
int a3 = A().k;  // { dg-error "context" }
int a4 = A().l;  // { dg-error "context" }
int a5 = A().m;  // { dg-error "context" }
int a6 = A().n;  // { dg-error "context" }
int a7 = A().o;  // { dg-error "context" }
