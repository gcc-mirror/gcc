Tue Oct  6 16:18:10 1998  Kaveh R. Ghazi  <ghazi@caip.rutgers.edu>

	* key-list.cc (output_keyword_blank_entries): Output
 	get_fill_default() in the blank entries of keywords as a trailing
	list of initializers.

	* options.cc: Add support for fill_default.

	* options.h: Likewise.
	
	* options.icc: Likewise.

	* version.cc: Update to indicate forked version.


diff -rup orig/gperf-2.7/src/key-list.cc gperf-2.7/src/key-list.cc
--- orig/gperf-2.7/src/key-list.cc	Wed Apr 15 18:02:51 1998
+++ gperf-2.7/src/key-list.cc	Tue Oct  6 15:38:54 1998
@@ -1069,7 +1069,7 @@ output_keyword_blank_entries (int count,
             printf (", ");
         }
       if (option[TYPE])
-        printf ("{\"\"}");
+        printf ("{\"\"%s}", option.get_fill_default());
       else
         printf ("\"\"");
       column++;
diff -rup orig/gperf-2.7/src/options.cc gperf-2.7/src/options.cc
--- orig/gperf-2.7/src/options.cc	Sat May  2 06:35:16 1998
+++ gperf-2.7/src/options.cc	Tue Oct  6 15:20:03 1998
@@ -40,6 +40,9 @@ static const int DEFAULT_JUMP_VALUE = 5;
 /* Default name for generated lookup function. */
 static const char *const DEFAULT_NAME = "in_word_set";
 
+/* Default filler for keyword table. */
+static const char *const DEFAULT_FILL = "";
+
 /* Default name for the key component. */
 static const char *const DEFAULT_KEY = "name";
 
@@ -66,6 +69,7 @@ int Options::argument_count;
 int Options::iterations;
 char **Options::argument_vector;
 const char *Options::function_name;
+const char *Options::fill_default;
 const char *Options::key_name;
 const char *Options::class_name;
 const char *Options::hash_name;
@@ -265,6 +269,7 @@ Options::Options (void)
   jump                = DEFAULT_JUMP_VALUE;
   option_word         = DEFAULTCHARS | C;
   function_name       = DEFAULT_NAME;
+  fill_default        = DEFAULT_FILL;
   key_name            = DEFAULT_KEY;
   hash_name           = DEFAULT_HASH_NAME;
   wordlist_name       = DEFAULT_WORDLIST_NAME;
@@ -306,6 +311,7 @@ Options::~Options (void)
                "\nSEVENBIT is....: %s"
                "\niterations = %d"
                "\nlookup function name = %s"
+               "\nfill default = %s"
                "\nhash function name = %s"
                "\nword list name = %s"
                "\nkey name = %s"
@@ -336,7 +342,7 @@ Options::~Options (void)
                option_word & INCLUDE ? "enabled" : "disabled",
                option_word & SEVENBIT ? "enabled" : "disabled",
                iterations,
-               function_name, hash_name, wordlist_name, key_name,
+               function_name, fill_default, hash_name, wordlist_name, key_name,
                jump, size - 1, initial_asso_value, delimiters, total_switches);
       if (option_word & ALLCHARS)
         fprintf (stderr, "all characters are used in the hash function\n");
@@ -379,6 +385,7 @@ static const struct option long_options[
   { "compare-strlen", no_argument, 0, 'l' },
   { "duplicates", no_argument, 0, 'D' },
   { "fast", required_argument, 0, 'f' },
+  { "fill-default", required_argument, 0, 'F' },
   { "initial-asso", required_argument, 0, 'i' },
   { "jump", required_argument, 0, 'j' },
   { "no-strlen", no_argument, 0, 'n' },
@@ -403,7 +410,7 @@ Options::operator() (int argc, char *arg
 
   while ((option_char =
             getopt_long (argument_count, argument_vector,
-                         "adcCDe:Ef:gGhH:i:Ij:k:K:lL:nN:oprs:S:tTvW:Z:7",
+                         "adcCDe:Ef:F:gGhH:i:Ij:k:K:lL:nN:oprs:S:tTvW:Z:7",
                          long_options, (int *)0))
          != -1)
     {
@@ -575,6 +582,11 @@ Options::operator() (int argc, char *arg
         case 'N':               /* Make generated lookup function name be optarg */
           {
             function_name = /*getopt*/optarg;
+            break;
+          }
+        case 'F':               /* Make fill_default be optarg */
+          {
+            fill_default = /*getopt*/optarg;
             break;
           }
         case 'o':               /* Order input by frequency of key set occurrence. */
diff -rup orig/gperf-2.7/src/options.h gperf-2.7/src/options.h
--- orig/gperf-2.7/src/options.h	Tue Apr 14 06:55:28 1998
+++ gperf-2.7/src/options.h	Tue Oct  6 15:12:46 1998
@@ -97,6 +97,7 @@ public:
   static int          initial_value (void);
   static int          get_total_switches (void);
   static const char  *get_function_name (void);
+  static const char  *get_fill_default (void);
   static const char  *get_key_name (void);
   static const char  *get_class_name (void);
   static const char  *get_hash_name (void);
@@ -115,6 +116,7 @@ private:
   static int          iterations;                         /* Amount to iterate when a collision occurs. */
   static char       **argument_vector;                    /* Stores a pointer to command-line vector. */
   static const char  *function_name;                      /* Names used for generated lookup function. */
+  static const char  *fill_default;                       /* Expression used to assign default values in keyword table. */
   static const char  *key_name;                           /* Name used for keyword key. */
   static const char  *class_name;                         /* Name used for generated C++ class. */
   static const char  *hash_name;                          /* Name used for generated hash function. */
diff -rup orig/gperf-2.7/src/options.icc gperf-2.7/src/options.icc
--- orig/gperf-2.7/src/options.icc	Sat Mar 21 07:51:17 1998
+++ gperf-2.7/src/options.icc	Tue Oct  6 15:27:36 1998
@@ -110,6 +110,14 @@ Options::get_function_name (void)
   return function_name;
 }
 
+/* Returns the fill default. */
+INLINE const char *
+Options::get_fill_default (void)
+{
+  T (Trace t ("Options::get_fill_default");)
+  return fill_default;
+}
+
 /* Returns the keyword key name. */
 INLINE const char *
 Options::get_key_name (void)
diff -rup orig/gperf-2.7/src/version.cc gperf-2.7/src/version.cc
--- orig/gperf-2.7/src/version.cc	Sat May  2 06:29:43 1998
+++ gperf-2.7/src/version.cc	Tue Oct  6 16:04:56 1998
@@ -19,4 +19,4 @@ You should have received a copy of the G
 along with GNU GPERF; see the file COPYING.  If not, write to the Free
 Software Foundation, 59 Temple Place - Suite 330, Boston, MA 02111, USA.  */
 
-const char *version_string = "2.7";
+const char *version_string = "2.7.1 (19981006 egcs)";
