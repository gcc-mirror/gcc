/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */
/* { dg-excess-errors "The error is sent to the SARIF file, rather than stderr" } */

struct s { int color; };

int test (struct s *ptr)
{
  return ptr->colour;
}

/* 
   { dg-final { verify-sarif-file } }

       { dg-final { scan-sarif-file "\"level\": \"error\"" } }

       We expect a logical location for the error (within fn "test"):
       { dg-final { scan-sarif-file "\"locations\": \\\[" } }
         { dg-final { scan-sarif-file "\"logicalLocations\": \\\[" } }
           { dg-final { scan-sarif-file "\"kind\": \"function\"" } }
           { dg-final { scan-sarif-file "\"name\": \"test\"" } }
           { dg-final { scan-sarif-file "\"fullyQualifiedName\": \"test\"" } }
           { dg-final { scan-sarif-file "\"decoratedName\": \"" } }

       We expect a "fixes" array for the fix-it hint (SARIF v2.1.0 section 3.27.30):
       { dg-final { scan-sarif-file "\"fixes\": \\\[" } }
         { dg-final { scan-sarif-file "\"artifactChanges\": \\\[" } }
           { dg-final { scan-sarif-file "\"replacements\": \\\[" } }
             { dg-final { scan-sarif-file "\"insertedContent\": " } }
               { dg-final { scan-sarif-file "\"text\": \"color\"" } }
             { dg-final { scan-sarif-file "\"deletedRegion\": " } }
*/
