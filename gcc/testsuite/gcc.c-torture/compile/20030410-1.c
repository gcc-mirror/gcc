/* PR 10201 */

extern struct _zend_compiler_globals compiler_globals;
typedef struct _zend_executor_globals zend_executor_globals;
extern zend_executor_globals executor_globals;

typedef struct _zend_ptr_stack {
        int top;
        void **top_element;
} zend_ptr_stack;
struct _zend_compiler_globals {
};
struct _zend_executor_globals {
        int *uninitialized_zval_ptr;
        zend_ptr_stack argument_stack;
};

static inline void safe_free_zval_ptr(int *p)
{
        if (p!=(executor_globals.uninitialized_zval_ptr)) {
        }
}
zend_executor_globals executor_globals;
static inline void zend_ptr_stack_clear_multiple(void)
{
        executor_globals.argument_stack.top -= 2;
}
