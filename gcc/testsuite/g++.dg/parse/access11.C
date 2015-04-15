// PR c++/24926

class A {
  union {
    int i;       // { dg-message "private" }
  };
  union {
    int j;       // { dg-message "private" }
  }; 
  union {
    union {
      int k;     // { dg-message "private" }
    };
    union {
      union {
	int l;   // { dg-message "private" }
      };
      union {
	int m;   // { dg-message "private" }
	union {
	  int n; // { dg-message "private" }
	  int o; // { dg-message "private" }
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
