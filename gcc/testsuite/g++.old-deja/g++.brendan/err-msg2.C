// Build don't link: 
// GROUPS passed error-messages
typedef void (*pfv)(double, double);
extern "C" typedef void (*pfv)(double, double);// ERROR -  multiple.*
