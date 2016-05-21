/* { dg-do compile } */
/* { dg-options "-O1" } */

struct ResponseStruct{                                                                                            
    unsigned char responseLength;
    char *response;
};

static char response[5];
struct ResponseStruct something(){
    struct ResponseStruct returnValue;
    returnValue.responseLength = 5;
    returnValue.response = response;
    return returnValue;
}

