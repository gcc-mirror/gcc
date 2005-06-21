typedef struct JSObject JSObject;
typedef struct JSObjectMap *(*JSNewObjectMapOp) (JSObject *obj);
typedef JSObject *(*JSGetMethodOp) (JSObject *obj);
struct JSObjectOps {
    JSNewObjectMapOp newObjectMap;
};
struct JSXMLObjectOps {
    struct JSObjectOps base;
    JSGetMethodOp getMethod;
};
struct JSObjectMap {
    struct JSObjectOps *ops;
};
struct JSObject {
    struct JSObjectMap *map;
};

struct JSXMLObjectOps js_XMLObjectOps;


/* We need to create SFT's for the entire structure when this address is taken, 
   not just the part in the component reference itself.  */
JSObject *JS_GetMethod(JSObject *obj)
{
    if (obj->map->ops == &js_XMLObjectOps.base) {
        struct JSXMLObjectOps *ops;
        ops = (struct JSXMLObjectOps *) obj->map->ops;
        obj = ops->getMethod(obj);
    }
    return obj;
}
