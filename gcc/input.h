/* Source file current line is coming from.  */
extern char *input_filename;

/* Top-level source file.  */
extern char *main_input_filename;

/* Line number in current source file.  */
extern int lineno;

/* Stream for reading from input file.  */
extern FILE *finput;

struct file_stack
  {
    char *name;
    struct file_stack *next;
    int line;
  };

/* Stack of currently pending input files.
   The line member is not accurate for the innermost file on the stack.  */
extern struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
extern int input_file_stack_tick;
