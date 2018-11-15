/* Test of cpp_callbacks::comments.  */

#include "gcc-plugin.h"
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cpplib.h"
#include "diagnostic.h"
#include "c-family/c-pragma.h"

int plugin_is_GPL_compatible;

/* Test callback for cpp_callbacks::comments.  */

void
my_comment_cb (cpp_reader *, location_t loc,
	       const unsigned char *content, size_t len)
{
  if (in_system_header_at (loc))
    return;

  /* CONTENT contains the opening slash-star (or slash-slash),
     and for C-style comments contains the closing star-slash.  */
  gcc_assert (len >= 2);
  gcc_assert (content[0] == '/');
  gcc_assert (content[1] == '*' || content[1] == '/');
  bool c_style = (content[1] == '*');
  if (c_style)
    {
      gcc_assert (content[len - 2] == '*');
      gcc_assert (content[len - 1] == '/');
    }

  if (c_style)
    inform (loc, "got C-style comment; length=%i", len);
  else
    inform (loc, "got C++-style comment; length=%i", len);

  /* Print the content of the comment.
     For a C-style comment, the buffer CONTENT contains the opening
     slash-star and closing star-slash, so we can't directly verify
     it in the DejaGnu test without adding another comment, which
     would trigger this callback again.
     Hence we skip the syntactically-significant parts of the comment
     when printing it.  */
  fprintf (stderr, "stripped content of comment: >");
  /* Avoid printing trailing star-slash.  */
  if (c_style)
    len -= 2;
  for (size_t i = 2; i < len; i++)
    fputc (content[i], stderr);
  fprintf (stderr, "<\n");
}

int
plugin_init (struct plugin_name_args *plugin_info,
	     struct plugin_gcc_version *version)
{
  cpp_callbacks *cb = cpp_get_callbacks (parse_in);
  cb->comment = my_comment_cb;

  return 0;
}
