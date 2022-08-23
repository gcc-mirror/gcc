// PR c++/102740
// { dg-do compile { target c++20 } }
// { dg-additional-options -Wmissing-braces }

typedef struct {
    union {
        struct {
            const void* content;
        } put;
    };
} op_t;

op_t f(const char* alias) {
    return op_t{
        .put =
            {
                .content = alias,
            },
    };				// { dg-warning "missing braces" }
}
