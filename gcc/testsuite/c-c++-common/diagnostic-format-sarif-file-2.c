/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file -Wmisleading-indentation" } */

int test (void)
{
  if (1)
    return 3;
    return 4;
  return 5;
}

/* 
   { dg-final { verify-sarif-file } }

       { dg-final { scan-sarif-file "\"level\": \"warning\"" } }
       { dg-final { scan-sarif-file "\"ruleId\": \"-Wmisleading-indentation\"" } }
         { dg-final { scan-sarif-file "\"text\": \"  if " } }

       { dg-final { scan-sarif-file "\"locations\": \\\[" } }

       We expect a logical location for the error (within fn "test"):
         { dg-final { scan-sarif-file "\"logicalLocations\": \\\[" } }
           { dg-final { scan-sarif-file "\"kind\": \"function\"" } }
           { dg-final { scan-sarif-file "\"name\": \"test\"" } }
           { dg-final { scan-sarif-file "\"fullyQualifiedName\": \"test\"" } }
           { dg-final { scan-sarif-file "\"decoratedName\": \"" } }

       We expect the "note" to become a "relatedLocations" entry:
       { dg-final { scan-sarif-file "\"relatedLocations\": \\\[" } }
         { dg-final { scan-sarif-file "\"text\": \"    return 4;" } }
*/
