/* Example of using a message buffer to build the text of a diagnostic
   in pieces before emitting it.  */

#include "libgdiagnostics.h"
#include "test-helpers.h"

int
main ()
{
  begin_test ("test-message-buffer.c.exe",
	      "test-message-buffer.c.sarif",
	      __FILE__, "c");

  diagnostic_event_id event_id = 0;

  /* begin quoted source */
  diagnostic *d = diagnostic_begin (diag_mgr,
				    DIAGNOSTIC_LEVEL_ERROR);

  diagnostic_message_buffer *msg_buf = diagnostic_message_buffer_new ();

  /* Add a null-terminated string.  */
  diagnostic_message_buffer_append_str (msg_buf, "this is a string; ");

  /* Add a length-specified string.  */
  diagnostic_message_buffer_append_text (msg_buf, "foobar", 3);

  /* "printf"-formatting.  */
  diagnostic_message_buffer_append_printf (msg_buf,
					   "; int: %i str: %s; ",
					   42, "mostly harmless");

  /* Adding a URL.  */
  diagnostic_message_buffer_begin_url (msg_buf, "https://example.com/");
  diagnostic_message_buffer_append_str (msg_buf, "this is a link");  
  diagnostic_message_buffer_end_url (msg_buf);

  diagnostic_message_buffer_append_str (msg_buf, " ");  

  /* Add quoted text.  */
  diagnostic_message_buffer_begin_quote (msg_buf);
  diagnostic_message_buffer_append_str (msg_buf, "this is quoted");  
  diagnostic_message_buffer_end_quote (msg_buf);
  
  diagnostic_message_buffer_append_str (msg_buf, " ");  

  /* Add colorized text.  */
  diagnostic_message_buffer_begin_color (msg_buf, "highlight-a");
  diagnostic_message_buffer_append_str (msg_buf, "highlight A");  
  diagnostic_message_buffer_end_color (msg_buf);

  diagnostic_message_buffer_append_str (msg_buf, " ");
  
  diagnostic_message_buffer_begin_color (msg_buf, "highlight-b");
  diagnostic_message_buffer_append_str (msg_buf, "highlight B");  
  diagnostic_message_buffer_end_color (msg_buf);
  
  diagnostic_message_buffer_append_str (msg_buf, " ");

  /* Add an event ID.  This will be printed as "(1)".  */
  diagnostic_message_buffer_append_event_id (msg_buf, event_id);
  
  /* Add an ASCII char.  */
  diagnostic_message_buffer_append_byte (msg_buf, '.');

  diagnostic_finish_via_msg_buf (d, msg_buf);
  /* end quoted source */

  return end_test ();
};

/* Verify the output from the text sink.
   { dg-regexp "test-message-buffer.c.exe: error: this is a string; foo; int: 42 str: mostly harmless; this is a link 'this is quoted' highlight A highlight B \\(1\\)." } */

/* Verify that some JSON was written to a file with the expected name:
   { dg-final { verify-sarif-file } } */

/* Use a Python script to verify various properties about the generated
   .sarif file:
   { dg-final { run-sarif-pytest test-message-buffer.c "test-message-buffer-c.py" } } */
