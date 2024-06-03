/* Verify behavior of SARIF output for the "no diagnostics" case.  */

/* { dg-do compile } */
/* { dg-options "-fdiagnostics-format=sarif-file" } */

int non_empty;

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

       We expect the contents of the file to *not* be quoted if
       there are no results.
       { dg-final { scan-sarif-file-not "\"contents\": " } }
         { dg-final { scan-sarif-file-not "\"text\": " } }
	 
       Verify that this file's "role" is "analysisTarget", as per
       "NOTE 3" in SARIF v2.1.0 section 3.24.6.
       { dg-final { scan-sarif-file "\"roles\": \\\[\"analysisTarget\"\\\]" } }

     { dg-final { scan-sarif-file "\"tool\": " } }
       { dg-final { scan-sarif-file "\"driver\": " } }
         { dg-final { scan-sarif-file "\"name\": \"GNU C" } }
         { dg-final { scan-sarif-file "\"fullName\": \"GNU C" } }
         { dg-final { scan-sarif-file "\"informationUri\": \"" } }

     { dg-final { scan-sarif-file "\"invocations\": \\\[" } }
       { dg-final { scan-sarif-file "\"toolExecutionNotifications\": \\\[\\\]" } }
       { dg-final { scan-sarif-file "\"executionSuccessful\": true" } }

     We expect an empty list for "results"
     { dg-final { scan-sarif-file "\"results\": \\\[\\\]" } }  */

