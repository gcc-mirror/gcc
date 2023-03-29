/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */
/* { dg-excess-errors "The error is sent to the SARIF file, rather than stderr" } */

int test (void)
{
  int 文字化け = *42;
}

/* 
   { dg-final { verify-sarif-file } }

       { dg-final { scan-sarif-file "\"level\": \"error\"" } }

       We expect the region expressed in display columns:
       { dg-final { scan-sarif-file "\"startLine\": 7" } }
       { dg-final { scan-sarif-file "\"startColumn\": 18" } }
       { dg-final { scan-sarif-file "\"endColumn\": 21" } }

       { dg-final { scan-sarif-file "\"text\": \"  int \\u6587\\u5b57\\u5316\\u3051 = " } }
*/
