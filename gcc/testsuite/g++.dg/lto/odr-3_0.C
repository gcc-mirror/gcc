// { dg-lto-do link }
// { dg-lto-options { -O0 -flto }  } 

typedef struct {
  int a; // { dg-lto-message "the first difference of corresponding definitions is field 'a'" }
} YYSTYPE; // { dg-lto-warning "3: warning: type ‘struct YYSTYPE’ violates the C\\+\\+ One Definition Rule" }
union yyalloc { // { dg-lto-warning "7: type ‘union yyalloc’ violates the C\\+\\+ One Definition Rule" }
  short yyss;
  YYSTYPE yyvs; // { dg-lto-message "the first difference of corresponding definitions is field ‘yyvs’" }
};
void b() { yyalloc c; }

