typedef struct YYSTYPE { // { dg-lto-message ":16 a different type is defined in another translation unit" }
} YYSTYPE;
union yyalloc { 
  short yyss;
  YYSTYPE yyvs; // { dg-lto-message "the first difference of corresponding definitions is field ‘yyvs’" }

};
void a() { yyalloc b; }

