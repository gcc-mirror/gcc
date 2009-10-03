typedef unsigned long uint32;
typedef int JSIntn;
#define JS_DLL_CALLBACK	
typedef JSIntn JSBool;
typedef struct JSContext JSContext;
typedef struct JSObject JSObject;
typedef long long JSInt64;
typedef JSInt64 JSWord;
typedef JSWord jsword;
typedef jsword jsval;

typedef JSBool
(* JS_DLL_CALLBACK JSPropertyOp)(JSContext *cx, JSObject *ojb, jsval id,
				 jsval *vp);

struct JSClass {
    const char *name;
    uint32 flags;
    JSPropertyOp addProperty;
};

extern struct JSClass K;
