/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

#warning message

/* Verify that some JSON was written to a file with the expected name.  */
/* { dg-final { verify-sarif-file } } */

/* We expect various properties.
   The indentation here reflects the expected hierarchy, though these tests
   don't check for that, merely the string fragments we expect.
   { dg-final { scan-sarif-file "\"version\": \"2.1.0\"" } }
   { dg-final { scan-sarif-file "\"runs\": \\\[" } }
     { dg-final { scan-sarif-file "\"artifacts\": \\\[" } } 
       { dg-final { scan-sarif-file "\"location\": " } }
         { dg-final { scan-sarif-file "\"uri\": " } }

       { dg-final { scan-sarif-file "\"sourceLanguage\": \"c\"" { target c } } }
       { dg-final { scan-sarif-file "\"sourceLanguage\": \"cplusplus\"" { target c++ } } }

       { dg-final { scan-sarif-file "\"contents\": " } }
         { dg-final { scan-sarif-file "\"text\": " } }
     { dg-final { scan-sarif-file "\"tool\": " } }
       { dg-final { scan-sarif-file "\"driver\": " } }
         { dg-final { scan-sarif-file "\"name\": \"GNU C" } }
         { dg-final { scan-sarif-file "\"fullName\": \"GNU C" } }
         { dg-final { scan-sarif-file "\"informationUri\": \"" } }

     { dg-final { scan-sarif-file "\"invocations\": \\\[" } }
       { dg-final { scan-sarif-file "\"toolExecutionNotifications\": \\\[\\\]" } }
       { dg-final { scan-sarif-file "\"executionSuccessful\": true" } }

     { dg-final { scan-sarif-file "\"results\": \\\[" } }
       { dg-final { scan-sarif-file "\"level\": \"warning\"" } }
       { dg-final { scan-sarif-file "\"ruleId\": \"-Wcpp\"" } }
       { dg-final { scan-sarif-file "\"locations\": \\\[" } }
         { dg-final { scan-sarif-file "\"physicalLocation\": " } }
           { dg-final { scan-sarif-file "\"contextRegion\": " } }
           { dg-final { scan-sarif-file "\"artifactLocation\": " } }
           { dg-final { scan-sarif-file "\"region\": " } }
             { dg-final { scan-sarif-file "\"startLine\": 4" } }
             { dg-final { scan-sarif-file "\"startColumn\": 2" } }
             { dg-final { scan-sarif-file "\"endColumn\": 9" } }

         We don't expect logical locations for a top-level warning:
         { dg-final { scan-sarif-file-not "\"logicalLocations\": " } }

       { dg-final { scan-sarif-file "\"message\": " } }
         { dg-final { scan-sarif-file "\"text\": \"#warning message" } } */
