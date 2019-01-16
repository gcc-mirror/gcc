// PERMUTE_ARGS:
// REQUIRED_ARGS: -D -Dd${RESULTS_DIR}/compilable -o-
// POST_SCRIPT: compilable/extra-files/ddocAny-postscript.sh 9155

module ddoc9155;

/++
 +  Note:
 +     test document note
 +     2nd line
 +  Example:
 +  ---
 +  import std.stdio;   //&
 +  writeln("Hello world!");
 +  if (test) {  
 +    writefln("D programming language");
 +  }
 +
 +      algorithm;
 +  
 +  xxx;    //comment
 +      yyy;
 +  /* test
 +   * comment
 +   */
 +
 + // Create MIME Base64 with CRLF, per line 76.
 +File f = File("./text.txt", "r");
 +uint line = 0;
 + // The ElementType of data is not aggregation type
 +foreach (encoded; Base64.encoder(data)) 
 +  ---
 +/

/**
  --------------------------------------------------------
  wstring ws;
  transcode("hello world",ws);
      // transcode from UTF-8 to UTF-16
      --------------------------------------------------------
 */

/**
 *  Example:
 *  ---
 *  import std.stdio;   //&
 *  writeln("Hello world!");
 *  if (test) {  
 *    writefln("D programming language");
 *  }
 *
 *      algorithm;
 *  
 *  xxx;    //comment
 *      yyy;
 *  /+ test
 *   + comment
 *   +/
 *  ---
 */

/**
----
#!/usr/bin/env rdmd
// Computes average line length for standard input.
import std.stdio;
----
*/

/**
	---
	writefln(q"EOS
	This
	is a multi-line
	heredoc string
	EOS"
	);
	---
*/

void foo(){}
